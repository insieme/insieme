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

#include "insieme/analysis/cba/framework/entities/program_point.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * A thread region is a path between two sync points within a thread. It is the basic
	 * unit for composing the petri net modeling the parallel execution of a application
	 * within the CBA framework.
	 */
	template<typename Context>
	class ThreadRegion : public utils::Printable {

		/**
		 * The beginning of the region.
		 */
		ProgramPoint<Context> begin;

		/**
		 * The end of the region.
		 */
		ProgramPoint<Context> end;

	public:

		ThreadRegion(const ProgramPoint<Context>& begin, const ProgramPoint<Context>& end)
			: begin(begin), end(end) {
			// make sure the start and end point is within the same thread
			assert_eq(begin.getContext().threadContext, end.getContext().threadContext);
		}


		const ProgramPoint<Context>& getBegin() const {
			return begin;
		}

		const ProgramPoint<Context>& getEnd() const {
			return end;
		}

		bool operator==(const ThreadRegion<Context>& other) const {
			if (this == &other) return true;
			return begin == other.begin && end == other.end;
		}

		bool operator<(const ThreadRegion<Context>& other) const {
			return begin < other.begin || (begin == other.begin && end < other.end);
		}

		/**
		 * Allows this object to be printed to any output stream (in a somewhat readable manner).
		 */
		std::ostream& printTo(std::ostream& out) const {
			return out << begin << " - " << end;
		}
	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
