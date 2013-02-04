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

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_expressions.h"

#include "insieme/core/ir_address.h"
#include "insieme/utils/printable.h"

#include "insieme/analysis/cfg.h"
#include "insieme/analysis/tmp_var_map.h"

#include <boost/variant.hpp>

namespace insieme { 
namespace analysis { 
namespace access {

	/**
	 * It abstract an address which can refer either to the IR (ir_address) or to the CFG. 
	 *
	 * An address in the IR is represented by the sequence of child indexes from the root node 
	 * while an address in the CFG is represented by a triple: cfg-block, stmt-idx, ir_address from
	 * the root. 
	 */
	struct UnifiedAddress : public utils::Printable {
	 
		typedef boost::variant<core::NodeAddress, cfg::Address> AddressImpl;
		
		/** 
		 * Builds an address starting from an ir-address
		 */
		UnifiedAddress(const core::NodeAddress& addr) : address(addr) { }

		/** 
		 * Builds an address starting from a CFG triple
		 */
		UnifiedAddress(const cfg::Address& addr) : address(addr) { }

		/** 
		 * Returns true when the address refers to the CFG
		 */
		bool isCFGAddress() const;

		core::NodeAddress getAbsoluteAddress(const TmpVarMap& varMap=TmpVarMap()) const;

		core::NodePtr getAddressedNode() const;

		UnifiedAddress getAddressOfChild(unsigned idx) const;
		
		UnifiedAddress extendAddressFor(const std::vector<unsigned>& idxs) const;

		template <class T>
		inline T as() const { return boost::get<T>(address); }

		bool operator==(const UnifiedAddress& other) const;

		inline std::ostream& printTo(std::ostream& out) const { return out << address; }

		inline operator bool() const {
			if (isCFGAddress()) { return static_cast<bool>(as<cfg::Address>()); }
			return as<core::NodeAddress>();
		}

	private:
		AddressImpl address;

	};

} // end access namespace
} // end analysis namespace 
} // end insieme namespace 
