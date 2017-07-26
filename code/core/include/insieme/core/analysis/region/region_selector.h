/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

#include <vector>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {
namespace analysis {
namespace region {

	using std::vector;


	/**
	 * A region is defined via begin and end statement addresses of a single-entry single-exit piece of code. begin and end can also be the same in case of
	 * representing a single statement.
	 */

	class Region : public utils::Printable {
		core::StatementAddress begin;
		core::StatementAddress end;

	  public:
		/**
		* Constructors for differing begin and end addresses, as well as a single address
		*/
		Region(const core::StatementAddress& begin, const core::StatementAddress& end) : begin(begin), end(end) {}

		Region(const core::StatementAddress& single) : begin(single), end(single) {}

		Region(const std::pair<core::StatementAddress, core::StatementAddress> region) : begin(region.first), end(region.second) {}

		Region(const Region& other) : begin(other.begin), end(other.end) {}

		core::StatementAddress getBegin() const { return begin; }

		core::StatementAddress getEnd() const { return end; }

		// sort according to addresses
		bool operator<(Region other) const { return std::tie(begin, end) < std::tie(other.begin, other.end); }

		bool operator==(Region other) const { return std::tie(begin, end) == std::tie(other.begin, other.end); }

		bool operator>(Region other) const { return other < *this; }

		bool operator>=(Region other) const { return !(*this < other); }

		bool operator<=(Region other) const { return !(other < *this); }

		bool operator!=(Region other) const { return !(other == *this); }

		virtual std::ostream& printTo(std::ostream& out) const { return out << "[" << begin << "," << end << "]"; }
	};

	typedef vector<Region> RegionList;

	/**
	 * An abstract base class defining the interface for any kind of region selection
	 * mechanism to be supported.
	 */
	class RegionSelector {
	  public:
		/**
		 * A virtual destructor for this abstract, virtual base class.
		 */
		virtual ~RegionSelector(){};

		/**
		 * This method is determining a list of regions within the given code fragment.
		 * The method represents the sole functionality of a region extractor. Implementations
		 * of this abstract base class have to provide corresponding implementations for
		 * this method.
		 *
		 * @param code the code fragment within which regions should be determined
		 * @return a list of addresses to the nodes forming the selected regions. The root
		 * 		of all obtained addresses has to be equivalent to the given code region.
		 */
		virtual RegionList getRegions(const core::NodeAddress& code) const = 0;
	};

} // end namespace region
} // end namespace analysis
} // end namespace core
} // end namespace insieme
