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

#include "insieme/analysis/dfa/analyses/def_use.h"

#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/analysis/dfa/entity.h"
#include "insieme/analysis/dfa/solver.h"

#include "insieme/analysis/polyhedral/scop.h"

namespace insieme { namespace analysis { namespace dfa { namespace analyses {

namespace {

void lookup_accesses(AddressSet& addrSet, const AccessClassSet& clSet, const CFGPtr& cfg) {

	for (auto& cl : clSet) {
		auto addrs = extractRealAddresses(*cl, cfg->getTmpVarMap());
		std::copy(addrs.begin(), addrs.end(), std::inserter(addrSet, addrSet.begin()));

		auto parent = cl->getParentClass();
//		if (parent) { 
//			bool found=false;
//			for(const auto& dep : parent->getSubClasses()) {
//				if (std::get<0>(dep) == AccessClass::DT_RANGE && 
//					std::get<1>(dep).lock() == cl) { found = true; }
//
//			}
	//		if (found) 
	//		lookup_accesses(addrSet, cl->getParentClass(), cfg); 
//		}
	}
}

AddressSet getDefinitions(
		const Solver<dfa::analyses::ReachingDefinitions>::CFGBlockMap& ret, 
		const CFGPtr& cfg, 
		const core::ExpressionAddress& use) 
{
	
	cfg::Address addr = cfg->find(use);
	auto blockID = addr.getBlock().getBlockID();

	AccessManager aMgr(&*cfg, cfg->getTmpVarMap());
	auto fit = ret.find(blockID);
	definitionsToAccesses(fit->second, aMgr);

	auto thisAccess = getImmediateAccess(use->getNodeManager(), use);

	AddressSet addrSet;
	lookup_accesses(addrSet, aMgr.getClassFor(thisAccess), cfg);

	// remove the address of the use 
	addrSet.erase(use);

	return addrSet;
}

} // end anonymous namespace 

struct DefUse::DefUseImpl {

	CFGPtr cfg;
	std::map<size_t, typename ReachingDefinitions::value_type> analysis;

	DefUseImpl(const core::NodePtr& root, const CFGPtr& cfg) : cfg(cfg)
	{
		if (!this->cfg) { this->cfg = CFG::buildCFG(root); }

		Solver<ReachingDefinitions> s(*this->cfg);
		
		// Collect polyhedral informations and attach it to the IR nodes 
		polyhedral::scop::mark(root);

		analysis = std::move(s.solve());
	}
	
};

DefUse::DefUse(const core::NodePtr& root, const CFGPtr& cfg) : 
	pimpl( std::make_shared<DefUse::DefUseImpl>(root, cfg) ) { }


AddressSet DefUse::getDefinitions(const core::ExpressionAddress& addr) {
	return insieme::analysis::dfa::analyses::getDefinitions(pimpl->analysis, pimpl->cfg, addr); 
}


} } } } // end insieme::analysis::dfa::analyses
