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

#include <boost/operators.hpp>

#include "insieme/core/pattern/pattern.h"
#include "insieme/core/pattern/generator.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace pattern {

	/**
	 * A symbolic variable representing trees within both, patterns and generators.
	 */
	class Variable :
			public boost::equality_comparable<Variable>,
			public boost::less_than_comparable<Variable>,
			public utils::Printable {

		/**
		 * The name of this variable.
		 */
		string name;

		/**
		 * The pattern version of this variable.
		 */
		TreePatternPtr pVar;

		/**
		 * The generator version of this variable.
		 */
		TreeGeneratorPtr gVar;

	public:

		Variable(const string& name, const TreePatternPtr& pattern = any)
			: name(name), pVar(var(name, pattern)), gVar(generator::var(name)) {}

		Variable(const char* name, const TreePatternPtr& pattern = any)
			: name(name), pVar(var(name, pattern)), gVar(generator::var(name)) {}

		/**
		 * The implicit conversion support to convert instances to tree patterns.
		 */
		operator const TreePatternPtr&() const {
			return pVar;
		}

		/**
		 * The implicit conversion support to convert instances to tree generators.
		 */
		operator const TreeGeneratorPtr&() const {
			return gVar;
		}

		bool operator==(const Variable& other) const {
			return this == &other || name == other.name;
		}

		bool operator<(const Variable& other) const {
			return name < other.name;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "$" << name;
		}

	};

	namespace detail {

		/**
		 * A utility class to aggregate lists of variables by the << operator before deciding
		 * whether it is representing a pattern or generator.
		 */
		class VarList {

			vector<Variable> vars;

		public:

			VarList(const Variable& a, const Variable& b)
				: vars(toVector(a,b)) {}

			VarList(const VarList& a, const Variable& b)
				: vars(a.vars) { vars.push_back(b); }

			VarList(const Variable& a, const VarList& b)
				: vars() { vars.push_back(a); vars.insert(vars.end(),b.vars.begin(), b.vars.end()); }

			VarList(const VarList& a, const VarList& b)
				: vars(a.vars) { vars.insert(vars.end(),b.vars.begin(), b.vars.end()); }

			operator ListPatternPtr() const {
				auto cur = vars.begin();
				auto res = single(*cur); ++cur;
				for(; cur != vars.end(); ++cur) {
					res = res << *cur;
				}
				return res;
			}

			operator ListGeneratorPtr() const {
				auto cur = vars.begin();
				auto res = generator::single(*cur); ++cur;
				for(; cur != vars.end(); ++cur) {
					res = res << *cur;
				}
				return res;
			}
		};

	}

	inline detail::VarList operator<<(const Variable& a, const Variable& b) {
		return detail::VarList(a, b);
	}

	inline detail::VarList operator<<(const detail::VarList& a, const Variable& b) {
		return detail::VarList(a, b);
	}

	inline detail::VarList operator<<(const Variable& a, const detail::VarList& b) {
		return detail::VarList(a, b);
	}

	inline detail::VarList operator<<(const detail::VarList& a, const detail::VarList& b) {
		return detail::VarList(a, b);
	}
} // end namespace pattern
} // end namespace core
} // end namespace insieme
