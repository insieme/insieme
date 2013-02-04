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

#include "insieme/analysis/access/access.h"
#include "insieme/analysis/access/access_mgr.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"

#include <algorithm>

namespace insieme { 
namespace analysis {
namespace dfa {

/**
 * Define the extractor for CFG Blocks. In this case we extract the address of
 * the CFG Blocks (an alternative would be to store the block ID)
 */
template <class T>
inline typename container_type_traits< dfa::elem<cfg::BlockPtr> >::type 
extract(const Entity< elem<cfg::BlockPtr> >& e, const CFG& cfg, T&) {
	
	typedef typename container_type_traits< dfa::elem<cfg::BlockPtr> >::type Container;

	Container entities;
	auto collector = [&entities] (const cfg::BlockPtr& block) { entities.insert( block ); };
	cfg.visitDFS(collector);

	return entities;
}

template <class T>
typename container_type_traits< dfa::elem< insieme::analysis::access::AccessClassPtr >  >::type 
extract(const Entity< dfa::elem<insieme::analysis::access::AccessClassPtr> >& e, const CFG& cfg, T& obj) {

	using namespace insieme::analysis::access;

	std::set<AccessClassPtr> entities;
	auto& aMgr = obj.getAccessManager();

	auto collector = [&] (const cfg::BlockPtr& block) {
		size_t stmt_idx=0;

		auto storeAccess = [&](const core::NodeAddress& var) {
		
			auto accesses = getAccesses(
								var->getNodeManager(), 
								cfg::Address(block, stmt_idx, var),
								cfg.getTmpVarMap()
							);

			for (const auto& acc : accesses) {
				auto classes = aMgr.getClassFor(acc);
				std::copy(classes.begin(), classes.end(), std::inserter(entities, entities.begin()));
			}
		};

		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {
			// Update the stmt_idx when returning from this lambda
			FinalActions fa([&](){ ++stmt_idx; });

			auto stmt = core::NodeAddress(cur.getAnalysisStatement());

			// The index access class has already been classified by the loop itself 
			if (cur.getType() == cfg::Element::LOOP_INCREMENT) { return; }

			storeAccess( stmt );

		});
	};
	cfg.visitDFS(collector);

	return entities;
}

} } } // end insieme::analysis::dfa::analyses namespace 
