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

#include <string>

#include "insieme/utils/annotation.h"
#include "insieme/core/ir_node.h"

namespace insieme {
namespace annotations {
namespace c {

/**
 * Annotation which contains the range within an element in the IR was defined.
 */
class IncludeFileAnnotation : public core::NodeAnnotation {
	const string file;
public:
	static const string NAME;
	static const utils::StringKey<IncludeFileAnnotation> KEY;

	IncludeFileAnnotation(const string& fileName): file(fileName) {}

	const std::string& getAnnotationName() const { return NAME; }
	const utils::AnnotationKeyPtr getKey() const { return &KEY; }

	std::ostream& printTo(std::ostream& out) const { return out << getAnnotationName() << ": " << file; }

	bool operator==(const IncludeFileAnnotation& other) const {
		if(this == &other)
			return true;
		return file == other.getFileName();
	}

	bool operator!=(const IncludeFileAnnotation& other) const {
		return !(*this==other);
	}


	const string& getFileName() const { return file; }
};

} // end namespace c_info
} // end namespace annotations
} // end namespace insieme
