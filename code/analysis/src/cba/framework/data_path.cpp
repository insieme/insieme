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

#include "insieme/analysis/cba/framework/data_path.h"

#include "insieme/core/ir.h"
#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {
namespace cba {

	// --------------------------------------------------------------------------------
	// 									Data Path Elements
	// --------------------------------------------------------------------------------


	enum DataPathElementKind {
		DPEK_MemberAccess,
		DPEK_ElementAccess,
		DPEK_ParentAccess
	};


	class DataPathElement {

		friend class DataPath;

		const DataPathElement* next;

		DataPathElementKind kind;

		mutable std::size_t refCount;

	public:

		DataPathElement(const DataPathElement* next) : next(next), refCount(0) {
			if (next) next->incRefCount();
		}

		~DataPathElement() {
			if (next) next->decRefCount();
		}

	private:

		void incRefCount() const {
			++refCount;
		}

		void decRefCount() const {
			assert_gt(refCount, 0);
			--refCount;
			if (refCount == 0) {
				delete this;
			}
		}
	};


	std::ostream& operator<<(std::ostream& out, const DataPathElement* element) {
		if (!element) {
			return out << "#";
		}
		assert_fail() << "Not implemented";
		return out << " - not implemented - ";
		// print recursively
//		return out << element->next << *element;
	}


	// --------------------------------------------------------------------------------
	// 										Data Path
	// --------------------------------------------------------------------------------

	DataPath::DataPath(DataPathElement* path) : path(path) {
		if (path) path->incRefCount();
	}

	DataPath::~DataPath() {
		if (path) path->decRefCount();
	}


	std::ostream& DataPath::printTo(std::ostream& out) const {
		return out << path;
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
