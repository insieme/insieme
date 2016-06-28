/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {

	class IntegerSet {

		std::set<int> elements;

		bool all;

		IntegerSet(bool all)
			: elements(), all(all) {}

	public:

		IntegerSet()
			: elements(), all(false) {}

		static IntegerSet getAll() {
			return IntegerSet(true);
		}

		bool empty() const {
			return !all && elements.empty();
		}

		std::size_t size() const {
			return (all) ? std::numeric_limits<std::size_t>::max() : elements.size();
		}

		const std::set<int> &getElements() const {
			return elements;
		}

		bool isUniversal() const {
			return all;
		}

		void insert(int a) {
			if (all) return;
			elements.insert(a);
 		}

		bool operator==(const IntegerSet &rhs) const {
			/* Caution: checks equality of sets only */
			return size() == rhs.size() && elements == rhs.getElements();
		}

		bool operator!=(const IntegerSet &rhs) const {
			/* Caution: checks inequality of sets only */
			return !(*this == rhs);
		}

		bool will_equal(const IntegerSet &rhs) const {
			return size() == 1 && *this == rhs;
		}

		bool may_equal(const IntegerSet &rhs) const {
			for (const int &mine : elements)
				for (const int &theirs : rhs.getElements())
					if (mine == theirs)
						return true;
			return false;
		}

		bool will_not_equal(const IntegerSet &rhs) const {
			for (const int &mine : elements)
				for (const int &theirs : rhs.getElements())
					if (mine == theirs)
						return false;
			return true;
		}

		friend std::ostream& operator<<(std::ostream& out, const IntegerSet& set) {
			if (set.all) {
				return out << "{-all-}";
			}
			return out << set.elements;
		}

	};

} // end namespace analysis
} // end namespace insieme

