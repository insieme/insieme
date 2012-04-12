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

#include <cassert>
#include <unordered_map>
#include <boost/functional/hash.hpp>

#include "insieme/utils/annotation.h"

#include "insieme/core/ir_node.h"

namespace insieme {
namespace annotations {
namespace c {

/** Annotation class intended to keep the name of C types and functions.
 ** Should be used to annotate *pointers* when exactly one name is required,
 ** for example with structs, unions or functions.
 ** */
class CNameAnnotation : public core::NodeAnnotation {
	const string name;

public:
	static const string NAME;
	static const utils::StringKey<CNameAnnotation> KEY;

	CNameAnnotation(const std::string& name) : core::NodeAnnotation(), name(name) { 
		assert(!name.empty() && "Name cannot be empty");
	}

	const std::string& getName() const { return name; }
	const std::string& getAnnotationName() const { return NAME; }

	std::ostream& printTo(std::ostream& out) const { return out << name; }

	const utils::AnnotationKey* getKey() const { return &KEY; }

	bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
		assert(&*ptr == this && "Annotation Pointer should point to this!");
		// always migrate the name annotation
		after->addAnnotation(ptr);
		return true;
	}
};

/**
 * A utility function simplifying the attachment of names to IR nodes.
 *
 * @param node the node the name should be attached to
 * @param name the name to be attached
 * @return the given node
 */
template<typename T>
const T& attachCName(const T& node, const string& name) {
	const core::NodePtr& cur = node;
	cur->addAnnotation<annotations::c::CNameAnnotation>(name);
	return node;
}

template<typename T>
T& copyCName(T& target, const core::NodePtr& src) {
	if (src->hasAnnotation(annotations::c::CNameAnnotation::KEY)) {
		target->addAnnotation(src->getAnnotation(annotations::c::CNameAnnotation::KEY));
	}
	return target;
}

} // end namespace c_info
} // end namespace annotations
} // end namespace insieme

