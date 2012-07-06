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

#include "insieme/core/forward_decls.h"

#include <iterator>

namespace insieme { namespace analysis { namespace dfa { namespace analyses {

/** 
 * forward decl for PIMPL pattern
 */
class DefUse {

	class DefUseImpl;

	std::shared_ptr<DefUseImpl> pimpl;

public:

	struct defs_iterator_impl;

	struct defs_iterator: std::iterator< std::forward_iterator_tag, core::VariableAddress > {
		
		core::VariableAddress operator*() const;

		defs_iterator& operator++() { inc(false); return *this; }

		bool operator==(const defs_iterator& other) const;

		bool operator!=(const defs_iterator& other) const { return !((*this) == other); }

	private:
		std::shared_ptr<defs_iterator_impl> pimpl;

		friend class DefUse;
		defs_iterator( const std::shared_ptr<defs_iterator_impl>& pimpl ) : pimpl(pimpl) { inc(true); } 
	
		void inc(bool first);
	};

	typedef defs_iterator iterator;

	DefUse(const core::NodePtr& root);

	defs_iterator defs_begin(const core::VariableAddress& var) const;

	defs_iterator defs_end(const core::VariableAddress& var) const;

};

} } } } // end insieme::analysis::dfa::analyses

