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
	class Job
		: public utils::Printable,
		  public utils::HashableImmutableData<Job<Context>>,
		  public boost::equality_comparable<Job<Context>>,
		  public boost::partially_ordered<Job<Context>> {

		/**
		 * The expression which created the represented channel.
		 */
		core::JobExprAddress creationPoint;

		/**
		 * The context when triggering the create function.
		 */
		Context creationContext;

	public:

		Job()
			: utils::HashableImmutableData<Job<Context>>(combineHashes(core::JobExprAddress(), Context())) {}

		Job(const core::JobExprAddress& job, const Context& ctxt)
			: utils::HashableImmutableData<Job<Context>>(combineHashes(job, ctxt)),
			  creationPoint(job),
			  creationContext(ctxt) {}

		const core::JobExprAddress& getAddress() const {
			return creationPoint;
		}

		const Context& getContext() const {
			return creationContext;
		}

		bool operator==(const Job<Context>& other) const {
			if (this == &other) return true;
			if (this->hash() != other.hash()) return false;
			return creationPoint == other.creationPoint && creationContext == other.creationContext;
		}

		bool operator<(const Job<Context>& other) const {
			return creationPoint < other.creationPoint ||
					(creationPoint == other.creationPoint && creationContext < other.creationContext);
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "job@" << creationPoint << "::" << creationContext;
		}
	};

	inline bool isJobConstructor(const core::ExpressionAddress& address) {
		return address.isa<core::JobExprAddress>();
	}

	template<typename Context>
	Job<Context> getJobFromConstructor(const core::ExpressionAddress& ctor, const Context& ctxt) {
		// make sure the target is a channel constructor
		assert(isJobConstructor(ctor));

		// create the job instance
		return Job<Context>(ctor.as<core::JobExprAddress>(), ctxt);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
