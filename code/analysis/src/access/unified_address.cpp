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

#include "insieme/analysis/access/unified_address.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/ir_address.h"

namespace {
	
	using namespace insieme;
	using namespace insieme::analysis::access;

	/** 
	 * Extracts the addressed node starting from an unified address 
	 */
	struct NodeExtractorVisitor : public boost::static_visitor<core::NodePtr> {
		template <class T>
		core::NodePtr operator()(const T& addr) const {
			return addr.getAddressedNode();
		}
	};

	struct AddrChildVisitor : public boost::static_visitor<UnifiedAddress> {

		unsigned idx;

		AddrChildVisitor(unsigned idx) : idx(idx) { }

		template <class T>
		UnifiedAddress operator()(const T& addr) const {
			return addr.getAddressOfChild(idx);
		}
	};

} // end anonymous namespace

namespace insieme {
namespace analysis {
namespace access {

bool UnifiedAddress::isCFGAddress() const {
	struct checkCFGAddrVisitor : public boost::static_visitor<bool> {
		bool operator()(const cfg::Address&) const { return true; }
		bool operator()(const core::NodeAddress&) const { return false; }
	};
	return boost::apply_visitor(checkCFGAddrVisitor(), address);
}


core::NodePtr UnifiedAddress::getAddressedNode() const {
	return boost::apply_visitor(NodeExtractorVisitor(), address);
}

core::NodeAddress UnifiedAddress::getAbsoluteAddress(const TmpVarMap& varMap) const {

	if (isCFGAddress()) {
		return boost::get<const cfg::Address&>(address).toAbsoluteAddress(varMap);
	}

	return boost::get<const core::NodeAddress&>(address);
}

UnifiedAddress UnifiedAddress::getAddressOfChild(unsigned idx) const {
	return boost::apply_visitor(AddrChildVisitor(idx), address);
}


UnifiedAddress UnifiedAddress::extendAddressFor(const std::vector<unsigned>& idxs) const {

	UnifiedAddress ret = *this;
	for (auto idx : idxs) {
		ret = ret.getAddressOfChild(idx);
	}
	return ret;

}


bool UnifiedAddress::operator==(const UnifiedAddress& other) const {
	
	if (this == &other) { return true; }

	if (isCFGAddress() == other.isCFGAddress()) {
		if (isCFGAddress()) {
			return as<cfg::Address>() == other.as<cfg::Address>();
		}
		return as<core::NodeAddress>() == other.as<core::NodeAddress>();
	}

	return false;
}

} // end access namespace  
} // end analysis namespace 
} // end insieme namespace 

