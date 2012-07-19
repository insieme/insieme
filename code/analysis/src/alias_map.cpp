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

#include "insieme/analysis/alias_map.h"
#include "insieme/analysis/dfa/analyses/ir_var_entity.h"

namespace insieme {
namespace analysis {

bool cmp_key::operator()(const core::ExpressionAddress& lhs, const core::ExpressionAddress& rhs) const {

	if (&lhs == &rhs) return false;

	if (lhs.getRootNode() < rhs.getRootNode()) return true;
	if (rhs.getRootNode() < lhs.getRootNode()) return false;
	
	assert(*lhs.getRootNode() == *rhs.getRootNode());
	return lhs.getPath() < rhs.getPath();
}

core::VariablePtr AliasMap::createAliasFor(const core::ExpressionAddress& expr) {

	auto alias = lookupImmediateAlias(expr);
	if (!alias) {
		core::IRBuilder builder(expr->getNodeManager());
		alias = builder.variable(expr->getType());
		aliasMap.insert( {expr, alias} );
	}

	return alias;
}

void AliasMap::storeAlias(const core::ExpressionAddress& expr, const core::VariablePtr& var) {

	auto alias = lookupImmediateAlias(expr);
	if (!alias) { aliasMap.insert( {expr, var} ); }
}

core::VariablePtr AliasMap::lookupImmediateAlias(const core::ExpressionAddress& expr) const { 
	auto&& fit = aliasMap.find(expr);
	if (fit == aliasMap.end()) {
		return core::VariablePtr();
	}
	return fit->second;
}

AliasMap::AliasSet AliasMap::lookupAliases(const core::ExpressionAddress& expr) const {

	AliasSet aliases;
	lookupAliasesImpl(expr, aliases);
	return aliases;
}

void AliasMap::lookupAliasesImpl(const core::ExpressionAddress& expr, AliasSet& aliases) const {
		
	dfa::analyses::VarEntity&& ve = dfa::analyses::makeVarEntity(expr);

	for (const auto& cur : aliasMap) {
		std::set<dfa::analyses::VarEntity>&& vars = dfa::analyses::extractFromStmt(cur.first);

		for(const auto& v : vars) {
			if (v.isLValue() && (ve == v)) { aliases.insert(cur.second); }
		}
	}
}

core::ExpressionAddress AliasMap::getMappedExpr(const core::VariablePtr& var) const {
	
	// FIXME: store an inverse map to speed this up
	auto fit = std::find_if(aliasMap.begin(), aliasMap.end(), 
			 	[&](const ExprToAliasMap::value_type& cur) { return *cur.second == *var; });

	if (fit != aliasMap.end()) return fit->first;
	return core::VariableAddress();

}


} // end analysis namespace
} // end insieme namespace 
