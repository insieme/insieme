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

#pragma once

#include <string>

#include <boost/operators.hpp>
#include <boost/variant.hpp>

#include "insieme/core/pattern/pattern.h"
#include "insieme/core/pattern/generator.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace pattern {

	namespace detail {
		// a utility function for creating variable names
		string getFreshVarName();
	}

	/**
	 * A symbolic variable representing trees within both, patterns and generators.
	 */
	class Variable : public boost::equality_comparable<Variable>, public boost::less_than_comparable<Variable>, public utils::Printable {
		/**
		 * The name of this variable.
		 */
		string name;

		/**
		 * The tree pattern version of this variable.
		 */
		TreePattern pVar;

		/**
		 * The generator version of this variable.
		 */
		TreeGenerator gVar;

	  public:
		Variable(const string& name = detail::getFreshVarName()) : name(name), pVar(var(name, any)), gVar(generator::var(name)) {}

		Variable(const string& name, const TreePattern& pattern) : name(name), pVar(var(name, pattern)), gVar(generator::var(name)) {}

		Variable(const char* name) : name(name), pVar(var(name, any)), gVar(generator::var(name)) {}

		Variable(const char* name, const TreePattern& pattern) : name(name), pVar(var(name, pattern)), gVar(generator::var(name)) {}

		explicit Variable(const TreePattern& pattern) : name(detail::getFreshVarName()), pVar(var(name, pattern)), gVar(generator::var(name)) {}

		Variable(const Variable& other) = default;

		Variable(Variable&& other) = default;

		/**
		 * The implicit conversion support to convert instances to tree patterns.
		 */
		operator const TreePattern&() const {
			return pVar;
		}

		/**
		 * The implicit conversion support to convert instances to tree generators.
		 */
		operator const TreeGenerator&() const {
			return gVar;
		}

		Variable& operator=(const Variable&) = default;

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

	/**
	 * A symbolic variable representing lists of trees within both, patterns and generators.
	 */
	class ListVariable : public boost::equality_comparable<ListVariable>, public boost::less_than_comparable<ListVariable>, public utils::Printable {
		/**
		 * The name of this variable.
		 */
		string name;

		/**
		 * The tree pattern version of this variable.
		 */
		ListPattern pVar;

		/**
		 * The generator version of this variable.
		 */
		ListGenerator gVar;

	  public:
		ListVariable(const string& name = detail::getFreshVarName()) : name(name), pVar(listVar(name, anyList)), gVar(generator::listVar(name)) {}

		ListVariable(const string& name, const ListPattern& pattern) : name(name), pVar(listVar(name, pattern)), gVar(generator::listVar(name)) {}

		ListVariable(const char* name) : name(name), pVar(listVar(name, anyList)), gVar(generator::listVar(name)) {}

		ListVariable(const char* name, const ListPattern& pattern) : name(name), pVar(listVar(name, pattern)), gVar(generator::listVar(name)) {}

		ListVariable(const ListVariable& other) = default;

		ListVariable(ListVariable&& other) = default;

		/**
		 * The implicit conversion support to convert instances to list patterns.
		 */
		operator const ListPattern&() const {
			return pVar;
		}

		/**
		 * The implicit conversion support to convert instances to list generators.
		 */
		operator const ListGenerator&() const {
			return gVar;
		}

		ListVariable& operator=(const ListVariable&) = default;

		bool operator==(const ListVariable& other) const {
			return this == &other || name == other.name;
		}

		bool operator<(const ListVariable& other) const {
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
			typedef boost::variant<Variable, ListVariable> var_type;

			vector<var_type> vars;

		  public:
			VarList() : vars() {}

			VarList(const VarList&) = default;

			VarList(VarList&&) = default;

			VarList& operator=(const VarList&) = default;

			VarList& append(const Variable& var) {
				vars.push_back(var);
				return *this;
			}

			VarList& append(const ListVariable& var) {
				vars.push_back(var);
				return *this;
			}

			VarList& append(const VarList& list) {
				vars.insert(vars.end(), list.vars.begin(), list.vars.end());
				return *this;
			}

			operator ListPattern() const {
				auto cur = vars.begin();
				auto res = toPattern(*cur);
				++cur;
				for(; cur != vars.end(); ++cur) {
					res = res << toPattern(*cur);
				}
				return res;
			}

			operator ListGenerator() const {
				auto cur = vars.begin();
				auto res = toGenerator(*cur);
				++cur;
				for(; cur != vars.end(); ++cur) {
					res = res << toGenerator(*cur);
				}
				return res;
			}

		  private:
			static ListPattern toPattern(const var_type& var) {
				struct list_converter : public boost::static_visitor<ListPattern> {
					ListPattern operator()(const Variable& var) const {
						return single(var);
					}
					ListPattern operator()(const ListVariable& var) const {
						return var;
					}
				};
				return boost::apply_visitor(list_converter(), var);
			}

			static ListGenerator toGenerator(const var_type& var) {
				struct list_converter : public boost::static_visitor<ListGenerator> {
					ListGenerator operator()(const Variable& var) const {
						return generator::single(var);
					}
					ListGenerator operator()(const ListVariable& var) const {
						return var;
					}
				};
				return boost::apply_visitor(list_converter(), var);
			}
		};
	}

	inline detail::VarList operator<<(const Variable& a, const Variable& b) {
		return detail::VarList().append(a).append(b);
	}

	inline detail::VarList operator<<(const Variable& a, const ListVariable& b) {
		return detail::VarList().append(a).append(b);
	}

	inline detail::VarList operator<<(const ListVariable& a, const Variable& b) {
		return detail::VarList().append(a).append(b);
	}

	inline detail::VarList operator<<(const ListVariable& a, const ListVariable& b) {
		return detail::VarList().append(a).append(b);
	}

	template <typename V>
	inline detail::VarList operator<<(const detail::VarList& a, const V& b) {
		return detail::VarList().append(a).append(b);
	}

	template <typename V>
	inline detail::VarList operator<<(detail::VarList&& a, const V& b) {
		return a.append(b);
	}


} // end namespace pattern
} // end namespace core
} // end namespace insieme
