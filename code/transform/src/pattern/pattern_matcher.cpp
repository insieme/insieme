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

#include "insieme/transform/pattern/pattern_matcher.h"

namespace insieme {
namespace transform {
namespace pattern {

	class MatchContext : public utils::Printable {

		typedef std::unordered_map<string, TreeList> NodeVarMap;
		typedef std::unordered_map<string, TreePatternPtr> RecVarMap;

		MatchPath path;

		Match match;

		RecVarMap boundRecursiveVariables;

	public:

		MatchContext() { }

		Match& getMatch() {
			return match;
		}

		const Match& getMatch() const {
			return match;
		}

		// -- The Match Path ---------------------------

		void push() {
			path.push(0);
		}

		void inc() {
			path.inc();
		}

		void dec() {
			path.dec();
		}

		void pop() {
			path.pop();
		}


		// -- Tree Variables ---------------------------

		bool isTreeVarBound(const std::string& var) const {
			return match.isTreeVarBound(path, var);
		}

		void bindTreeVar(const std::string& var, const TreePtr value) {
			match.bindTreeVar(path, var, value);
		}

		TreePtr getTreeVarBinding(const std::string& var) const {
			return match.getTreeVarBinding(path, var);
		}

		// -- Node Variables --------------------------

		bool isNodeVarBound(const std::string& var) const {
			return match.isListVarBound(path, var);
		}

		void bindNodeVar(const std::string& var, const TreeListIterator& begin, const TreeListIterator& end) {
			match.bindListVar(path, var, begin, end);
		}

		TreeList getNodeVarBinding(const std::string& var) const {
			return match.getListVarBinding(path, var);
		}

		// -- Recursive Variables ---------------------------

		bool isRecVarBound(const std::string& var) const {
			return boundRecursiveVariables.find(var) != boundRecursiveVariables.end();
		}

		void bindRecVar(const std::string& var, const TreePatternPtr& pattern) {
			assert(!isRecVarBound(var) && "Variable bound twice");
			boundRecursiveVariables.insert(RecVarMap::value_type(var, pattern));
		}

		TreePatternPtr getRecVarBinding(const std::string& var) const {
			assert(isRecVarBound(var) && "Requesting bound value for unbound tree variable");
			return boundRecursiveVariables.find(var)->second;
		}

		void unbindRecVar(const std::string& var) {
			boundRecursiveVariables.erase(var);
		}

		virtual std::ostream& printTo(std::ostream& out) const;
	};


	template<typename T>
	Match<T> match(const TreePatternPtr& pattern, const typename T::value_type& value);


} // end namespace pattern
} // end namespace transform
} // end namespace insieme
