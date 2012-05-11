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

#include "insieme/transform/pattern/structure.h"
#include "insieme/transform/pattern/pattern.h"
#include "insieme/transform/pattern/generator.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace transform {
namespace pattern {

	/**
	 * A rule consisting of a pattern to be matched and a generator rule
	 * producing the replacement for the matched structure.
	 */
	class Rule : public utils::Printable {

		TreePatternPtr pattern;
		TreeGeneratorPtr generator;

	public:

		Rule(const TreePatternPtr& pattern = any, const TreeGeneratorPtr& generator = generator::root)
			: pattern(pattern), generator(generator) {}

		core::NodePtr applyTo(const core::NodePtr& tree) const;

		// for testing only ...
		TreePtr applyTo(const TreePtr& tree) const;

		virtual std::ostream& printTo(std::ostream& out) const {
			return pattern->printTo(out) << " -> " << *generator;
		}
	};

    inline core::NodePtr apply(const core::NodePtr& node, const TreePatternPtr& pattern, const TreeGeneratorPtr& generator) {
        return Rule(pattern, generator).applyTo(node);
    }


} // end namespace pattern
} // end namespace transform
} // end namespace insieme
