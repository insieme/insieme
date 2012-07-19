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

#include "insieme/analysis/dfa/entity.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/datapath/datapath.h"

#include "insieme/utils/printable.h"

namespace insieme { namespace analysis { namespace dfa { namespace analyses {

enum class VarType { VAR, MEMBER, TUPLE, ARRAY };

class VarEntity : public utils::Printable {

	core::ExpressionAddress 	base_expr;
	core::datapath::DataPathPtr	path;
	VarType 					type;
	bool						rvalue;
	
	VarEntity(const core::ExpressionAddress& expr, 
			  const core::datapath::DataPathPtr& path, 
			  const VarType& type,
			  bool rvalue) : 
		base_expr(expr), 
		path(path), 
		type(type),
		rvalue(rvalue) { }

	friend VarEntity makeVarEntity(const core::ExpressionAddress& expr);
	friend std::set<VarEntity> extractFromStmt(const core::StatementPtr& stmt);

public:
	
	inline const VarType& getType() const { return type; }

	/**
	 * Tells whtether this usage of the variable is an rvalue.
	 * This is happens for example when a ref<'a> is dereferenced, or the type 
	 * of the variable is a non ref. 
	 */
	bool isRValue() const { return rvalue; }
	bool isLValue() const { return !isRValue(); }

	core::ExpressionPtr getBaseExpression() const { return base_expr; }

	std::ostream& printTo(std::ostream& out) const { 
		out << *base_expr; 
		if (path) { out << "!" << path; }
		return out << " { rvalue?:" << std::boolalpha << isRValue() << " }";
	}

	bool operator<(const VarEntity& other) const {
		if (base_expr.getAddressedNode() < other.base_expr.getAddressedNode())
			return true;
		
		if (base_expr.getAddressedNode() > other.base_expr.getAddressedNode())
			return false;

		return false; // path < other.path;
	}

	bool operator==(const VarEntity& other) const {
		return *base_expr.getAddressedNode() == *other.base_expr.getAddressedNode() && 
			   path == other.path;
	}

	bool operator!=(const VarEntity& other) const {
		return !(*this == other);
	}

};

std::set<VarEntity> extractFromStmt(const core::StatementPtr& stmt);

/**
 * Builds a VarEntity from an expression. 
 */
inline VarEntity makeVarEntity(const core::ExpressionAddress& expr) {
	std::set<VarEntity>&& entities = extractFromStmt(expr);

	assert(entities.size() == 1);
	return *entities.begin();
}

} // end analyses namespace 

template <>
inline typename container_type_traits< dfa::elem<analyses::VarEntity> >::type
extract(const Entity< dfa::elem<analyses::VarEntity> >& e, const CFG& cfg) { 

	std::set<analyses::VarEntity> entities;

	auto collector = [&entities] (const cfg::BlockPtr& block) {

		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {

			std::set<analyses::VarEntity> newEntities = 
				analyses::extractFromStmt( cur.getAnalysisStatement() );

			std::copy(newEntities.begin(), newEntities.end(), std::inserter(entities, entities.begin()));
		});

	};

	cfg.visitDFS(collector);

	return entities;

}


} } } // end insieme::analysis::dfa namespace 
