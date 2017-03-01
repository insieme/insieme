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
 *
 */
#include "insieme/core/annotations/error.h"

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"

#include "insieme/core/encoder/encoder.h"

namespace insieme {
namespace core {
namespace annotations {

	/**
	 * The value annotation type to be attached to nodes to  mark those conatining semantic errors
	 *
	 */
	struct ErrorTag : public value_annotation::cloneable {
		string msg;
		ErrorTag(const string& msg) : msg(msg) {}
		bool operator==(const ErrorTag& other) const {
			return msg == other.msg;
		}

		void cloneTo(const NodePtr& target) const {
			attachError(target, msg);
		}
	};

	// ----------------------------------------------------

	bool hasAttachedError(const NodePtr& node) {
		return node->hasAttachedValue<ErrorTag>();
	}

	const string& getAttachedError(const NodePtr& node) {
		assert_true(hasAttachedError(node)) << "Does not have an error annotation!";
		return node->getAttachedValue<ErrorTag>().msg;
	}

	void attachError(const NodePtr& node, const string& msg) {
		node->attachValue(ErrorTag(msg));
	}

} // end namespace annotations
} // end namespace core
} // end namespace insieme
