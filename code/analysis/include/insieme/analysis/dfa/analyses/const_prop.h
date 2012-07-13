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
#include "insieme/analysis/dfa/problem.h"

namespace insieme { namespace analysis { namespace dfa { namespace analyses {

/**
 * Define the DataFlowProblem for Constant Propagation
 */
class ConstantPropagation: 
	public Problem<ConstantPropagation, 
				   ForwardAnalysisTag,
				   Entity<dfa::elem<core::VariablePtr>, dfa::dom<dfa::Value<core::LiteralPtr>>>,
				   PowerSet
		   > 
{

	typedef Problem<ConstantPropagation, 
				   ForwardAnalysisTag,
				   Entity<dfa::elem<core::VariablePtr>, dfa::dom<dfa::Value<core::LiteralPtr>>>,
				   PowerSet
		   >  Base;
	
public:

	typedef typename Base::value_type value_type;

	ConstantPropagation(const CFG& cfg): Base(cfg) { }

	virtual value_type init() const { return top(); }

	virtual value_type top() const { 
		const auto& lhsBase = extracted.getLeftBaseSet();
		return makeCartProdSet(
				lhsBase, 
				std::set<dfa::Value<core::LiteralPtr>>( { dfa::Value<core::LiteralPtr>(dfa::top) } ) 
			).expand();
	}

	virtual value_type bottom() const {
		const auto& lhsBase = extracted.getLeftBaseSet();
		return makeCartProdSet(
				lhsBase, 
				std::set<dfa::Value<core::LiteralPtr>>( { dfa::Value<core::LiteralPtr>(dfa::bottom) } ) 
			).expand();
	}

	value_type meet(const value_type& lhs, const value_type& rhs) const;


	value_type transfer_func(const value_type& in, const cfg::BlockPtr& block) const;
};

} } } } // end insieme::analysis::dfa::analyses namespace 

