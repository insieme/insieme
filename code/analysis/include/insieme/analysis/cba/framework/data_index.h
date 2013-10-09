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

#include <cstdint>

#include <boost/operators.hpp>

#include "insieme/utils/printable.h"
#include "insieme/core/arithmetic/arithmetic.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * A base class for indices to be utilized for analyzing array operations
	 * within data values.
	 */
	template<typename DerivedIndex>
	struct Index :
			public boost::equality_comparable<DerivedIndex>,
			public boost::partially_ordered<DerivedIndex>,
			public utils::Printable
	{};

	/**
	 * The simplest form of an index - there is only a single instance, all array operations are
	 * applied to the same instance.
	 */
	class UnitIndex : public Index<UnitIndex> {

	public:

		UnitIndex() {}
		UnitIndex(const core::arithmetic::Formula& f) {}

		bool operator==(const UnitIndex& other) const {
			return true;	// all instances are equivalent
		}

		bool operator<(const UnitIndex& other) const {
			return false;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "unit";
		}

	};

	/**
	 * An index distinguishing concrete indices (if the value of a subscript is known) from cases
	 * where the index is unknown or too complex to be evaluated statically.
	 */
	class SingleIndex : public Index<SingleIndex> {

		/**
		 * A flag determining whether this instances is representing a concrete index
		 * or an unknown index.
		 */
		bool concrete;

		/**
		 * The represented index - if it is known.
		 */
		std::uint64_t index;

	public:

		SingleIndex() : concrete(false), index(0) {}

		SingleIndex(std::uint64_t index) : concrete(true), index(index) {}

		SingleIndex(const core::arithmetic::Formula& f)
			: concrete(f.isInteger()), index((concrete)?f.getIntegerValue():0) {}

		bool operator==(const SingleIndex& other) const {
			if (this == &other) return true;
			return concrete == other.concrete && index == other.index;
		}

		bool operator<(const SingleIndex& other) const {
			if (!concrete) return false;
			if (!other.concrete) return true;
			return index < other.index;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			if (!concrete) return out << "*";
			return out << index;
		}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
