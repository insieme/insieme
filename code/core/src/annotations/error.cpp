/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
struct ErrorTag : public  value_annotation::cloneable {
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
