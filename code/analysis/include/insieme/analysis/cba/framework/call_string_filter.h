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

#include <set>
#include <map>

#include "insieme/analysis/cba/framework/_forward_decl.h"

namespace insieme {
namespace analysis {
namespace cba {

	// a forward declaration for the CBA class.
	class CBA;
	class Callee;

	class CallStringFilter {

		CBA& cba;

		/**
		 * All labels to be encountered within call context strings.
		 */
		std::set<Label> allCallsSet;

		/**
		 * The same as the allCallsSet but in list form.
		 */
		std::vector<Label> allCallsList;

		/**
		 * A map of labels to valid predecessors.
		 */
		std::map<Label, std::set<Label>> predecessors;

	public:

		CallStringFilter(CBA& cba);

		const std::set<Label>& getAllPotentialPredecessors(const Label& label);

		const std::vector<Label>& getAllCallStringEntries() const {
			return allCallsList;
		}

		bool isValidCallStringEntry(const Label& l) {
			return allCallsSet.find(l) != allCallsSet.end();
		}

		bool isValidPredecessor(const Label& pre, const Label& pos) {
			const std::set<Label>& set = getAllPotentialPredecessors(pos);
			return set.find(pre) != set.end();
		}

	private:

		std::set<Label> computePotentialPredecessors(const Label& label);

		std::set<Label> computePotentialPredecessors(const StatementAddress& stmt);

		std::set<Label> getAllStaticUses(const Callee& fun);
	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
