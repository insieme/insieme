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

#include <set>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/datapath/datapath.h"

#include "insieme/utils/printable.h"

#include "insieme/analysis/alias_map.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme { 
namespace analysis { 

enum class VarType { SCALAR, MEMBER, TUPLE, ARRAY };

/** 
 * The Access class represent an access to an IR variable. 
 *
 * This could be a scalar, array, tuple or member variable access. 
 *
 * Because we want to represent the concept of V L-Values, this class can hold both RefTypes and not
 * ref types, therefore anything which can appear of the left hand side of an assignment operation
 * in C or a declaration statement. 
 */
class Access : public utils::Printable {

	// Address of the access represented by this object 
	core::ExpressionAddress 	base_expr;

	// Actuall variable being accessed (note that this variable might be an alias)
	core::VariablePtr			variable;

	// Path to the accessed member/element/component 
	//  => For scalar, the path is empty 
	core::datapath::DataPathPtr	path;

	// The type of this access
	VarType 					type;

    /**
     * Represents the domain/range on which this access is defined
     *
     * For scalars or members the domain the domain is not defined
	 *
     * For arrays the domain depends on the range of values being accessed. It could be either a
     * single element or strided domain in the circumstances the array is accessed inside a loop
     */
	//boost::optional<polyhedral::IterationDomain> dom;

	/**
	 * An iteration domain is meaningless if the associated SCoP is not specified. Indeed two accesses
	 * to the same variable with the same symbolic range are the same only if the relative iteration
	 * domain is within the same SCoP. If the SCoPs differs then we cannot make any assumption on
	 * the fact that the same range of elements is going to be accessed. 
	 */
	// boost::optional<const polyhedral::Scop&> 	scop;

	Access(const core::ExpressionAddress& 		expr, 
		   const core::VariablePtr& 			var,
		   const core::datapath::DataPathPtr& 	path, 
		   const VarType& 						type,
		   const boost::optional<polyhedral::IterationDomain>& 	dom,
		   const boost::optional<const polyhedral::Scop&>&		scop) : 
		base_expr(expr), 
		variable(var),
		path(path), 
		type(type)
		//dom(dom)
		//scop(scop) 
		{ }

	friend Access makeAccess(const core::ExpressionAddress& expr);

public:
	
	inline const VarType& getType() const { return type; }

	/**
	 * Tells whtether this usage of the variable is an rvalue.
	 * This is happens for example when a ref<'a> is dereferenced, or the type 
	 * of the variable is a non ref. 
	 */
	bool isRef() const;

	inline core::VariablePtr getAccessedVariable() const { 
		return variable; 
	}

	inline core::ExpressionAddress getAccessExpression() const { 
		return base_expr; 
	}

	inline const core::datapath::DataPathPtr& getPath() const {
		return path; 
	}

	std::ostream& printTo(std::ostream& out) const;

	inline bool operator<(const Access& other) const {
		if (variable < other.variable) { return true; }
		
		if (variable > other.variable) { return false; }

		return path < other.path;
	}

	inline bool operator==(const Access& other) const {
		return *variable == *other.variable;
	}

	inline bool operator!=(const Access& other) const {
		return !(*this == other);
	}

};

Access makeAccess(const core::ExpressionAddress& expr);

std::set<Access> extractFromStmt(const core::StatementAddress& stmt);

void extractFromStmt(const core::StatementAddress& stmt, std::set<Access>& accesses);

/**
 * States whether two accesses are conflicting, it returns true if the 2 accesses referes to the
 * same variable, and the datapaths of the two variables are overlapping. The predicate also
 * receives the alias map as argument so that it includes aliasing when checking for conflicts
 */
bool isConflicting(const Access& acc1, const Access& acc2, const AliasMap& aliases = AliasMap());

} } // end insieme::analysis::dfa namespace 
