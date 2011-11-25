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

#include <vector>
#include "insieme/transform/transformation.h"

#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/pattern/ir_generator.h"
#include "insieme/transform/pattern/rule.h"

namespace insieme {
namespace transform {

	namespace p = pattern;
	namespace g = pattern::generator;
	namespace irp = pattern::irp;
	namespace irg = pattern::generator::irg;

	/**
	 * A class realizing a transformation based on a set of transformation rules.
	 * The list of rules is scanned until one of the rules is matching. The transformed
	 * code will then be returned. If no rule is matching, no transformation will
	 * be applied.
	 */
	class RuleBasedTransformation : public Transformation {

		/**
		 * The set of rules to be tested.
		 */
		vector<pattern::Rule> rules;

	public:

		/**
		 * A constructor allowing to specify an arbitrary number of rules.
		 *
		 * @param rules the set of rules to be used by the resulting transformation
		 */
		template<typename ... Rules>
		RuleBasedTransformation(const Rules& ... rules)
			: rules(toVector<pattern::Rule>(rules...)) {}

		/**
		 * A constructor accepting a list of rules.
		 *
		 * @param rules the set of rules to be used by the resulting transformation
		 */
		RuleBasedTransformation(const vector<pattern::Rule>& rules)
			: rules(rules) {}


		/**
		 * Implements the actual transformation by scanning through the internally
		 * stored list of rules.
		 *
		 * @param target the node to be transformed
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const {
			// the first matching rule will be applied
			for(auto it = rules.begin(); it != rules.end(); ++it) {
				core::NodePtr res = it->applyTo(target);
				if (res) {
					return res;
				}
			}
			return target;
		}
	};



	/**
	 * A simple example transformation based on rules. This transformation is eliminating
	 * superfluous brackets / compound statement nodes.
	 */
	struct CompoundElimination : public RuleBasedTransformation {

		CompoundElimination()
			: RuleBasedTransformation(
					pattern::Rule(  		// {{x}} => {x}
							irp::compoundStmt(irp::compoundStmt(p::listVar("stmts"))),
							irg::compoundStmt(g::listVar("stmts"))
					),
					pattern::Rule(			// {x...x {} y...y} => {x...x,y...y}
							irp::compoundStmt(p::listVar("before") << irp::compoundStmt() << p::listVar("after")),
							irg::compoundStmt(g::listVar("before") << g::listVar("after"))
					)
		) {};
	};


} // end namespace transform
} // end namespace insieme
