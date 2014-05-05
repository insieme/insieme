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

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/hash_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	// the type to represent jobs
	template<typename Context>
	class ThreadBody
		: public utils::Printable,
		  public utils::HashableImmutableData<ThreadBody<Context>>,
		  public boost::equality_comparable<ThreadBody<Context>>,
		  public boost::partially_ordered<ThreadBody<Context>> {

		/**
		 * The statement (compound or call of a bind expression) forming the body
		 * of a thread.
		 */
		core::StatementInstance body;

		/**
		 * The root context of the processed thread.
		 */
		Context threadRootContext;

	public:

		ThreadBody()
			: utils::HashableImmutableData<ThreadBody<Context>>(combineHashes(core::StatementInstance(), Context())) {}

		ThreadBody(const core::StatementInstance& body, const Context& ctxt)
			: utils::HashableImmutableData<ThreadBody<Context>>(combineHashes(body, ctxt)),
			  body(body),
			  threadRootContext(ctxt) {}

		const core::StatementInstance& getBody() const {
			return body;
		}

		const Context& getContext() const {
			return threadRootContext;
		}

		bool operator==(const ThreadBody<Context>& other) const {
			if (this == &other) return true;
			if (this->hash() != other.hash()) return false;
			return body == other.body && threadRootContext == other.threadRootContext;
		}

		bool operator<(const ThreadBody<Context>& other) const {
			return body < other.body ||
					(body == other.body && threadRootContext < other.threadRootContext);
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "body@" << body << "::" << threadRootContext;
		}
	};

	template<typename Context>
	bool isSameThreadTeam(const ThreadBody<Context>& a, const ThreadBody<Context>& b) {
		// the body has to be the same
		if (a.getBody() != b.getBody()) return false;

		// if the context is equal => also the same
		if (a.getContext() == b.getContext()) return true;

		// they are also in the same team if they are only different in the thread ID
		Context cA = a.getContext();
		Context cB = b.getContext();

		// cancel out the thread ID
		cA.threadContext[0].id = 0;
		cB.threadContext[0].id = 0;
		return cA == cB;
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
