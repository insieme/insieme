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

#include <map>
#include <set>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {

struct cmp_key {

bool operator()(const insieme::core::ExpressionAddress& lhs, const insieme::core::ExpressionAddress& rhs) const;

};

/**
 * This class stores mappings between IR addresses and temporary variables which are introduced
 * during the construction of the CFG. 
 */
class TmpVarMap : public utils::Printable {

public:

	typedef std::map<core::ExpressionAddress, core::VariablePtr, cmp_key> 	ExprToAliasMap;
	typedef std::map<core::VariablePtr, core::ExpressionAddress> 			InvExprToAliasMap;

	TmpVarMap() { }

	/** 
	 * Given the address of an expression, we create a variable with the scope of storing the value
	 * of that expression 
	 */
	core::VariablePtr createTmpFor(const core::ExpressionAddress& expr);

	/** 
	 * Given a tuple (expr, var) we store it in the tmp var map for lookup
	 */
	void storeTmpVar(const core::ExpressionAddress& expr, const core::VariablePtr& var);

	core::VariablePtr lookupImmediateAlias(const core::ExpressionAddress& expr) const;

	/** 
	 * Given an expression, returns a set of variables which are aliases for that expression 
	 */
	// AliasSet lookupAliases(const core::ExpressionAddress& expr) const;

	/** 
	 * Given a temporary variable (or alias) this function returns the expression being mapped to
	 * that alias.
	 */
	core::ExpressionAddress getMappedExpr(const core::VariablePtr& var) const;

	inline bool isTmpVar(const core::VariablePtr& var) const {
		return invAliasMap.find(var) != invAliasMap.end();
	}

	/** 
	 * Returns true when the map is empty
	 */
	inline bool empty() const { return aliasMap.empty(); }

	std::ostream& printTo(std::ostream& out) const {
		return out << aliasMap;
	}

private:

	void addMappingImpl(const core::ExpressionAddress& expr, const core::VariablePtr& var);

	ExprToAliasMap aliasMap;
	InvExprToAliasMap invAliasMap;

	// void lookupAliasesImpl(const core::ExpressionAddress& expr, AliasSet& aliases) const;

};


} // end analysis namespace 
} // end insieme namespace 
