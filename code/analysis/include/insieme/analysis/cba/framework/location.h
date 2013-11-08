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

#include "insieme/analysis/cba/framework/data_path.h"

namespace insieme {
namespace analysis {
namespace cba {

	// the type to represent memory locations
	template<typename Context>
	class Location
		: public utils::Printable,
		  public utils::HashableImmutableData<Location<Context>>,
		  public boost::equality_comparable<Location<Context>>,
		  public boost::partially_ordered<Location<Context>> {

		/**
		 * The expression which created the represented memory location.
		 */
		core::ExpressionAddress creationPoint;

		/**
		 * The context when triggering the create function.
		 */
		Context creationContext;

	public:

		Location()
			: utils::HashableImmutableData<Location<Context>>(combineHashes(core::ExpressionAddress(), Context())) {}

		Location(const core::ExpressionAddress& expr, const Context& ctxt)
			: utils::HashableImmutableData<Location<Context>>(combineHashes(expr, ctxt)),
			  creationPoint(expr),
			  creationContext(ctxt) {}

		const core::ExpressionAddress& getAddress() const {
			return creationPoint;
		}

		const Context& getContext() const {
			return creationContext;
		}

		bool operator==(const Location<Context>& other) const {
			if (this == &other) return true;
			if (this->hash() != other.hash()) return false;
			return creationPoint == other.creationPoint && creationContext == other.creationContext;
		}

		bool operator<(const Location<Context>& other) const {
			return creationPoint < other.creationPoint ||
					(creationPoint == other.creationPoint && creationContext < other.creationContext);
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "(" << creationPoint << "," << creationContext << ")";
		}
	};

	template<typename Context>
	class Reference
			: public utils::Printable,
			  public utils::HashableImmutableData<Reference<Context>>,
			  public boost::equality_comparable<Reference<Context>>,
			  public boost::partially_ordered<Reference<Context>> {

		Location<Context> location;

		DataPath path;

	public:

		Reference()
			: utils::HashableImmutableData<Reference<Context>>(combineHashes(Location<Context>(),DataPath())) {}

		Reference(const Location<Context>& loc, const DataPath& path = DataPath())
			: utils::HashableImmutableData<Reference<Context>>(combineHashes(loc, path)),
			  location(loc),
			  path(path) {}

		const Location<Context>& getLocation() const {
			return location;
		}

		const DataPath& getDataPath() const {
			return path;
		}

		bool isAlias(const Reference& other) const {
			// it needs to be the same location
			if (location != other.location) return false;

			// and the path must be overlapping
			return path.isOverlapping(other.path);
		}

		bool operator==(const Reference<Context>& other) const {
			if (this == &other) return true;
			if (this->hash() != other.hash()) return false;
			return location == other.location && path == other.path;
		}

		bool operator<(const Reference<Context>& other) const {
			return location < other.location ||
					(location == other.location && path < other.path);
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "(" << location.getAddress() << "," << location.getContext() << "," << path << ")";
		}
	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
