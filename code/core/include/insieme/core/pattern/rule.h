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
#pragma once

#include "insieme/core/pattern/structure.h"
#include "insieme/core/pattern/pattern.h"
#include "insieme/core/pattern/generator.h"
#include "insieme/core/pattern/variable.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace pattern {

	/**
	 * A rule consisting of a pattern to be matched and a generator rule
	 * producing the replacement for the matched structure.
	 */
	class Rule : public utils::Printable {
		TreePattern pattern;
		TreeGenerator generator;

	  public:
		Rule(const TreePattern& pattern = any, const TreeGenerator& generator = generator::root) : pattern(pattern), generator(generator) {}

		/**
		 * Applies this rule to the given input node.
		 * If the rule does not fit a null pointer will be returned.
		 */
		core::NodePtr applyTo(const core::NodePtr& tree) const;

		/**
		 * Applies this rule to one of the sub-nodes of the given input node.
		 * If the rule does not fit a null pointer will be returned.
		 */
		core::NodePtr applyToNested(const core::NodePtr& tree) const {
			return getNestedRule().applyTo(tree);
		}

		/**
		 * Applies this rule to the given input node.
		 * If the rule does not fit a null pointer will be returned.
		 */
		core::NodePtr operator()(const core::NodePtr& tree) const {
			return applyTo(tree);
		}

		/**
		 * Applies this rule until a fixpoint is reached.
		 */
		core::NodePtr fixpoint(const core::NodePtr& tree) const;

		/**
		 * Applies this rule to all nested sub-structures until a fixpoint is reached.
		 */
		core::NodePtr fixpointNested(const core::NodePtr& tree) const {
			return getNestedRule().fixpoint(tree);
		}

		/**
		 * Obtains a rule equivalent to this rule, yet targeting an arbitrarily nested sub-structure.
		 */
		Rule getNestedRule() const;

		std::ostream& printTo(std::ostream& out) const {
			return out << pattern << " -> " << generator;
		}

		// for testing only ...
		TreePtr applyTo(const TreePtr& tree) const;
	};

	inline core::NodePtr apply(const core::NodePtr& node, const TreePattern& pattern, const TreeGenerator& generator) {
		return Rule(pattern, generator).applyTo(node);
	}


} // end namespace pattern
} // end namespace core
} // end namespace insieme
